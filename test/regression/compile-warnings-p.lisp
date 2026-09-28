;;; COMPILE returns warnings-p and failure-p (CLHS): a WARNING signaled while
;;; compiling, for instance by a macro's expander, makes warnings-p true, and one
;;; that is not a STYLE-WARNING makes failure-p true as well. COMPILE used to
;;; return NIL NIL whatever happened. serapeum's EIF warns this way when the else
;;; branch is missing, and its tests check the second value.

(defmacro cwp-warns (x)
  (warn "cwp-warns: ~s" x)
  x)

(define-condition cwp-style (style-warning) ())

(defmacro cwp-style-warns (x)
  (warn (quote cwp-style))
  x)

(deftest compile-warnings-p.warning
  (let ((*error-output* (make-broadcast-stream)))
    (multiple-value-bind (fn warnings-p failure-p)
        (compile nil '(lambda (y) (cwp-warns y)))
      (list (funcall fn 7) (and warnings-p t) (and failure-p t))))
  (7 t t))

(deftest compile-warnings-p.style-warning
  (let ((*error-output* (make-broadcast-stream)))
    (multiple-value-bind (fn warnings-p failure-p)
        (compile nil '(lambda (y) (cwp-style-warns y)))
      (list (funcall fn 8) (and warnings-p t) failure-p)))
  (8 t nil))

(deftest compile-warnings-p.clean
  (multiple-value-bind (fn warnings-p failure-p)
      (compile nil '(lambda (y) (1+ y)))
    (list (funcall fn 1) warnings-p failure-p))
  (2 nil nil))

(deftest compile-warnings-p.named
  (let ((*error-output* (make-broadcast-stream)))
    (multiple-value-list (compile 'cwp-named '(lambda () (cwp-warns 3)))))
  (cwp-named t t))
