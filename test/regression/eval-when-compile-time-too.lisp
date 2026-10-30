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

;; In compile-time-too mode a form is evaluated first and then compiled
;; (CLHS 3.2.3.1), so what the form itself does at compile time is seen by the
;; macros in it when it is compiled for the fasl. The code that was run at
;; compile time used to be reused for the fasl, expanded before the run. lift's
;; DEFTESTSUITE has this shape: one form records the suite's slot names and
;; defines the :SETUP method, whose body expands against those names. SBCL:
;; (:SET).
(deftest-compiled-only eval-when-compile-time-too.evaluate-then-compile
  (progn
    (setf (get 'ewctt4-key 'ewctt4-mark) nil)
    (%ewctt-compile-and-load
     '("(defmacro ewctt4-use () `',(get 'ewctt4-key 'ewctt4-mark))"
       "(eval-when (:compile-toplevel :load-toplevel :execute)"
       "  (let ()"
       "    (setf (get 'ewctt4-key 'ewctt4-mark) '(:set))"
       "    (defun ewctt4-seen () (ewctt4-use))))"))
    (funcall (intern "EWCTT4-SEEN")))
  (:set))

;;; A nested EVAL-WHEN is not top level: only :EXECUTE matters, both when the
;;; outer EVAL-WHEN is EVALUATEd ({:COMPILE-TOPLEVEL} without
;;; :LOAD-TOPLEVEL) and when its body is PROCESSed (with :LOAD-TOPLEVEL). CLHS
;;; 3.2.3.1 Figure 3-7 and EVAL-WHEN examples 4 and 5: A, B and D do nothing,
;;; C and E run once at compile time, and none of them reaches the fasl.
(defvar *ewn-sits* nil)

(deftest-compiled-only eval-when-nested.compile-only-outer-evaluates
  (let ((src "ewn-tmp.lisp")
        (fasl "ewn-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(eval-when (:compile-toplevel)" s)
             (write-line "  (eval-when (:compile-toplevel) (push :a *ewn-sits*)))" s)
             (write-line "(eval-when (:compile-toplevel)" s)
             (write-line "  (eval-when (:load-toplevel) (push :b *ewn-sits*)))" s)
             (write-line "(eval-when (:compile-toplevel)" s)
             (write-line "  (eval-when (:execute) (push :c *ewn-sits*)))" s)
             (write-line "(eval-when (:compile-toplevel)" s)
             (write-line "  (eval-when (:compile-toplevel :load-toplevel) (push :d *ewn-sits*)))" s)
             (write-line "(eval-when (:compile-toplevel)" s)
             (write-line "  (eval-when (:compile-toplevel :load-toplevel :execute) (push :e *ewn-sits*)))" s)
             (write-line "(push :loaded *ewn-sits*)" s))
           (setq *ewn-sits* nil)
           (compile-file src :output-file fasl)
           (let ((at-compile (nreverse *ewn-sits*)))
             (setq *ewn-sits* nil)
             (load fasl)
             (list at-compile (nreverse *ewn-sits*))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))
      (setq *ewn-sits* nil)))
  ((:c :e) (:loaded)))

;; The Process action still treats the body forms as top level: the nested
;; {:COMPILE-TOPLEVEL} is EVALUATEd at compile time and not put in the fasl.
(deftest-compiled-only eval-when-nested.compile-only-inside-compile-and-load
  (let ((src "ewn2-tmp.lisp")
        (fasl "ewn2-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(eval-when (:compile-toplevel :load-toplevel)" s)
             (write-line "  (eval-when (:compile-toplevel) (push :inner-ct *ewn-sits*)))" s))
           (setq *ewn-sits* nil)
           (compile-file src :output-file fasl)
           (let ((at-compile (nreverse *ewn-sits*)))
             (setq *ewn-sits* nil)
             (load fasl)
             (list at-compile (nreverse *ewn-sits*))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))
      (setq *ewn-sits* nil)))
  ((:inner-ct) nil))
