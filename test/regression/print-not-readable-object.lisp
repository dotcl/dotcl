;;; PRINT-NOT-READABLE-OBJECT must return the object that could not be printed.
;;;
;;; It was a stub: it checked its argument count and returned NIL, so a handler
;;; had no way to ask what failed -- the only trace was the message text. The
;;; PRINT-NOT-READABLE class has had an OBJECT slot all along; nothing filled it,
;;; and the conditions the printer raises are native (not CLOS instances), so
;;; they had nowhere to put it either.

(defun %pnro-object (thunk)
  "The object of the PRINT-NOT-READABLE THUNK signals, or :no-error."
  (handler-case (progn (funcall thunk) :no-error)
    (print-not-readable (e) (print-not-readable-object e))
    (error (e) (list :other (type-of e)))))

;;; A specialized array cannot be printed readably: #(...) syntax loses the
;;; element type. The object must be that very array, not a copy.
(deftest print-not-readable-object.specialized-array
  (let ((a (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
    (eq a (%pnro-object (lambda () (let ((*print-readably* t)) (prin1-to-string a))))))
  t)

(deftest print-not-readable-object.float-array
  (let ((a (make-array 2 :element-type 'single-float :initial-element 0.0)))
    (eq a (%pnro-object (lambda () (let ((*print-readably* t)) (prin1-to-string a))))))
  t)

;;; Nesting past the printer's depth guard: readable printing must fail rather
;;; than elide. The object here is the sub-structure printing stopped at -- the
;;; cons sitting at the depth limit, which is the one that could not be printed
;;; -- so the test looks for it inside the structure rather than at its root.
(defun %pnro-reachable-p (root obj)
  (do ((p root (car p)) (n 0 (1+ n)))
      ((or (eq p obj) (not (consp p)) (> n 400)) (eq p obj))))

(deftest print-not-readable-object.too-deep
  (let* ((x (let ((y 'a)) (dotimes (i 300 y) (setq y (list y)))))
         (obj (%pnro-object (lambda () (let ((*print-readably* t)) (prin1-to-string x))))))
    (list (consp obj) (%pnro-reachable-p x obj)))
  (t t))

;;; An infinity with *READ-EVAL* off has no readable form at all.
(deftest print-not-readable-object.non-finite-float
  (let ((inf dotcl:double-float-positive-infinity))
    (eql inf (%pnro-object (lambda ()
                             (let ((*print-readably* t) (*read-eval* nil))
                               (prin1-to-string inf))))))
  t)

;;; A condition built from Lisp keeps its OBJECT slot, and the accessor reads it.
(deftest print-not-readable-object.from-lisp
  (list (%pnro-object (lambda () (error 'print-not-readable :object 42)))
        (print-not-readable-object (make-condition 'print-not-readable :object :x)))
  (42 :x))

;;; The condition still reports itself sensibly.
(deftest print-not-readable-object.report
  (handler-case (error 'print-not-readable :object 42)
    (print-not-readable (e) (princ-to-string e)))
  "The object 42 cannot be printed readably.")

;;; Printing that does not ask for readability is unaffected.
(deftest print-not-readable-object.ordinary-printing-unaffected
  (let ((a (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
    (list (stringp (prin1-to-string a))
          (stringp (let ((*print-readably* nil)) (prin1-to-string a)))))
  (t t))
