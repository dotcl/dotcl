;;; COMPILE-FILE processes the body of (eval-when (:compile-toplevel
;;; :load-toplevel ...) ...) in compile-time-too mode (CLHS 3.2.3.1): each top
;;; level form in it is evaluated at compile time as it is processed, so its
;;; effect is in place before the next form is macroexpanded.
;;;
;;; The body used to be compiled as a whole and only then evaluated. A macro
;;; that expands into (progn (register ...) (use-registration ...)) under such
;;; an EVAL-WHEN saw nothing registered: the registration had been compiled but
;;; not yet run when the second form was expanded. cepl's DEFSTRUCT-G has this
;;; shape (varjo registers a type name, then a macro looks the name up).

(defun %ewctt-compile-and-load (lines)
  (let ((src "ewctt-tmp.lisp")
        (fasl "ewctt-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (dolist (l lines) (write-line l s)))
           (compile-file src :output-file fasl)
           (load fasl))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl)))))

(deftest-compiled-only eval-when-compile-time-too.form-by-form
  (progn
    (%ewctt-compile-and-load
     '("(eval-when (:compile-toplevel :load-toplevel :execute)"
       "  (defmacro ewctt-use () `',(get 'ewctt-key 'ewctt-mark))"
       "  (defmacro ewctt-define (v)"
       "    `(progn (setf (get 'ewctt-key 'ewctt-mark) ,v)"
       "            (defparameter *ewctt-seen* (ewctt-use)))))"
       "(eval-when (:compile-toplevel :load-toplevel :execute)"
       "  (ewctt-define 42))"))
    (symbol-value (intern "*EWCTT-SEEN*")))
  42)

;; :COMPILE-TOPLEVEL alone evaluates the body as EVAL would: a form at a time.
(deftest-compiled-only eval-when-compile-time-too.compile-only-body
  (progn
    (%ewctt-compile-and-load
     '("(eval-when (:compile-toplevel :load-toplevel :execute)"
       "  (defmacro ewctt2-use () `',(get 'ewctt2-key 'ewctt2-mark)))"
       "(eval-when (:compile-toplevel)"
       "  (setf (get 'ewctt2-key 'ewctt2-mark) :early)"
       "  (setf (get 'ewctt2-key 'ewctt2-copy) (macroexpand '(ewctt2-use))))"
       "(defparameter *ewctt2-seen* (list (ewctt2-use) (get 'ewctt2-key 'ewctt2-copy)))"))
    (symbol-value (intern "*EWCTT2-SEEN*")))
  (:early ':early))

;; A nested (eval-when (:load-toplevel) ...) inside compile-time-too mode goes
;; to not-compile-time mode (Figure 3-7): not evaluated at compile time.
(deftest-compiled-only eval-when-compile-time-too.nested-load-only
  (progn
    (setf (get 'ewctt3-key 'ewctt3-mark) nil)
    (%ewctt-compile-and-load
     '("(eval-when (:compile-toplevel :load-toplevel :execute)"
       "  (defmacro ewctt3-use () `',(get 'ewctt3-key 'ewctt3-mark))"
       "  (eval-when (:load-toplevel)"
       "    (setf (get 'ewctt3-key 'ewctt3-mark) :loaded))"
       "  (defparameter *ewctt3-seen* (ewctt3-use)))"))
    (list (symbol-value (intern "*EWCTT3-SEEN*")) (get 'ewctt3-key 'ewctt3-mark)))
  (nil :loaded))
