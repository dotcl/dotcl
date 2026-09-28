;;; A macro lambda list and a destructuring lambda list must reject an argument
;;; list of the wrong length.
;;;
;;; CLHS 3.4.4 and 3.4.5: a missing required element, or elements left over
;;; when there is no &REST, &BODY or &KEY to take them, is an error. dotcl bound
;;; a missing element to NIL and dropped the extra ones, so (TWO 1) for a macro
;;; of (A B) expanded into code with NIL in it and a misused macro went
;;; unnoticed at compile time. SBCL signals a PROGRAM-ERROR in each case.
;;;
;;; A compiler macro whose lambda list does not fit a call now signals while
;;; expanding; the compiler reports that as a warning and compiles the call
;;; as an ordinary call, as SBCL does.

(defun %dac (thunk)
  (handler-case (funcall thunk)
    (program-error () :program-error)
    (error (e) (list :other (type-of e)))))

(defmacro dac-two (a b) (list 'quote (list a b)))
(defmacro dac-nested ((a b)) (list 'quote (list a b)))
(defmacro dac-opt (a &optional b) (list 'quote (list a b)))
(defmacro dac-body (a &body b) (list 'quote (list a b)))

;;; --- DEFMACRO

(deftest destructuring-arg-count.macro-too-few
  (%dac (lambda () (macroexpand-1 '(dac-two 1))))
  :program-error)

(deftest destructuring-arg-count.macro-too-many
  (%dac (lambda () (macroexpand-1 '(dac-two 1 2 3))))
  :program-error)

(deftest destructuring-arg-count.macro-dotted
  (%dac (lambda () (macroexpand-1 '(dac-two 1 . 2))))
  :program-error)

(deftest destructuring-arg-count.macro-eval
  (%dac (lambda () (eval '(dac-two 1))))
  :program-error)

(deftest destructuring-arg-count.macro-function
  (%dac (lambda () (funcall (macro-function 'dac-two) '(dac-two 1) nil)))
  :program-error)

(deftest destructuring-arg-count.macro-nested-too-few
  (%dac (lambda () (macroexpand-1 '(dac-nested (1)))))
  :program-error)

(deftest destructuring-arg-count.macro-nested-too-many
  (%dac (lambda () (macroexpand-1 '(dac-nested (1 2 3)))))
  :program-error)

(deftest destructuring-arg-count.macro-nested-not-a-list
  (%dac (lambda () (macroexpand-1 '(dac-nested 1))))
  :program-error)

(deftest destructuring-arg-count.macro-optional-too-many
  (%dac (lambda () (macroexpand-1 '(dac-opt 1 2 3))))
  :program-error)

(deftest destructuring-arg-count.macro-optional-too-few
  (%dac (lambda () (macroexpand-1 '(dac-opt))))
  :program-error)

(deftest destructuring-arg-count.macro-ok
  (list (macroexpand-1 '(dac-two 1 2))
        (macroexpand-1 '(dac-nested (1 2)))
        (macroexpand-1 '(dac-opt 1))
        (macroexpand-1 '(dac-opt 1 2))
        (macroexpand-1 '(dac-body 1 2 3)))
  ('(1 2) '(1 2) '(1 nil) '(1 2) '(1 (2 3))))

(deftest destructuring-arg-count.macrolet-too-many
  (%dac (lambda () (eval '(macrolet ((m (a) a)) (m 1 2)))))
  :program-error)

;;; --- DESTRUCTURING-BIND

(deftest destructuring-arg-count.db-too-few
  (%dac (lambda () (destructuring-bind (a b) (list 1) (list a b))))
  :program-error)

(deftest destructuring-arg-count.db-too-many
  (%dac (lambda () (destructuring-bind (a b) (list 1 2 3) (list a b))))
  :program-error)

(deftest destructuring-arg-count.db-empty
  (%dac (lambda () (destructuring-bind (a b c) nil (list a b c))))
  :program-error)

(deftest destructuring-arg-count.db-not-a-list
  (%dac (lambda () (destructuring-bind (a b) 'x (list a b))))
  :program-error)

(deftest destructuring-arg-count.db-nested-too-few
  (%dac (lambda () (destructuring-bind ((a b)) (list (list 1)) (list a b))))
  :program-error)

(deftest destructuring-arg-count.db-optional-too-many
  (%dac (lambda () (destructuring-bind (a &optional b) (list 1 2 3) (list a b))))
  :program-error)

(deftest destructuring-arg-count.db-aux-too-many
  (%dac (lambda () (destructuring-bind (a &aux (b 2)) (list 1 9) (list a b))))
  :program-error)

(deftest destructuring-arg-count.db-ok
  (list (destructuring-bind (a b) (list 1 2) (list a b))
        (destructuring-bind (a &optional (b 5)) (list 1) (list a b))
        (destructuring-bind (a . b) (list 1 2 3) (list a b))
        (destructuring-bind (a &rest b) (list 1) (list a b))
        (destructuring-bind ((a b) c) (list (list 1 2) 3) (list a b c))
        (destructuring-bind (a &aux (b 2)) (list 1) (list a b)))
  ((1 2) (1 5) (1 (2 3)) (1 nil) (1 2 3) (1 2)))

;;; --- LOOP destructuring stays lenient (CLHS 6.1.1.7)

(deftest destructuring-arg-count.loop-lenient
  (list (loop for (a b) in '((1 2 3) (4)) collect (list a b))
        (loop with (a b) = '(1 2 3) return (list a b))
        (loop with (a b c) = '(1) return (list a b c)))
  (((1 2) (4 nil)) (1 2) (1 nil nil)))

;;; --- a compiler macro that does not fit the call falls back to the call

(define-compiler-macro dac-cm (a) (list 'quote (list :expanded a)))
(defun dac-cm (&rest r) r)

(deftest-compiled-only destructuring-arg-count.compiler-macro-fallback
  (let ((warned nil))
    (handler-bind ((warning (lambda (c) (setq warned t) (muffle-warning c))))
      (let ((fn (compile nil '(lambda () (list (dac-cm 1) (dac-cm 1 2))))))
        (list (funcall fn) warned))))
  (((:expanded 1) (1 2)) t))
