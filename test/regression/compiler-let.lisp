;;; DOTCL-CLTL2:COMPILER-LET binds while the body is EXPANDED, not at run time.
;;;
;;; CLtL1 5.3.2: the bindings are in effect during the processing of the body,
;;; so a macro in the body can read a variable the surrounding code set, and
;;; nothing is bound when the compiled code runs. The two halves are what make
;;; it useful and what make it different from LET.
;;;
;;; It expanded to LET, which got both halves backwards: the macro in the body
;;; expanded before the binding existed, and the binding was live at run time.
;;; SERIES is the library that noticed -- it wraps five forms in
;;; (compiler-let ((*optimize-series-expressions* nil)) ...) and reads that
;;; variable from inside a macro expander.
;;;
;;; Every expected value below was checked against sb-cltl2:compiler-let.

(defvar *clet-flag* :outer)
(defvar *clet-n* 0)

;; The pair the semantics turns on: one reads the variable while expanding, the
;; other reads it while running.
(defmacro clet-at-expansion () (list 'quote *clet-flag*))
(defun clet-at-runtime () *clet-flag*)

(defmacro clet-n-at-expansion () *clet-n*)

;;; ---- the two halves ----

;; SBCL: (:INNER :OUTER). Expanding to LET gives (:OUTER :INNER) -- both wrong.
(deftest compiler-let.expansion-sees-it-runtime-does-not
  (dotcl-cltl2:compiler-let ((*clet-flag* :inner))
    (list (clet-at-expansion) (clet-at-runtime)))
  (:inner :outer))

;; Nothing is bound at run time, and the global is untouched afterwards.
(deftest compiler-let.no-runtime-binding
  (list (dotcl-cltl2:compiler-let ((*clet-flag* :x)) (clet-at-runtime))
        *clet-flag*)
  (:outer :outer))

;;; ---- shapes of the binding list ----

(deftest compiler-let.nests
  (dotcl-cltl2:compiler-let ((*clet-flag* :a))
    (list (clet-at-expansion)
          (dotcl-cltl2:compiler-let ((*clet-flag* :b)) (clet-at-expansion))
          (clet-at-expansion)))
  (:a :b :a))

;; The value form is evaluated, in the expanding image, before the walk.
(deftest compiler-let.value-forms-are-evaluated
  (dotcl-cltl2:compiler-let ((*clet-n* (+ 1 2)))
    (clet-n-at-expansion))
  3)

;; A bare variable binds NIL rather than signalling (SBCL answers NIL).
(deftest compiler-let.bare-variable-binds-nil
  (dotcl-cltl2:compiler-let ((*clet-flag*))
    (clet-at-expansion))
  nil)

(deftest compiler-let.several-bindings
  (dotcl-cltl2:compiler-let ((*clet-flag* :both) (*clet-n* 7))
    (list (clet-at-expansion) (clet-n-at-expansion)))
  (:both 7))

;; An empty binding list is a plain body.
(deftest compiler-let.empty-bindings
  (dotcl-cltl2:compiler-let ()
    (list (clet-at-expansion) (clet-at-runtime)))
  (:outer :outer))

;; The body is a body: several forms, and the last one's value is the value.
(deftest compiler-let.body-is-a-progn
  (let ((seen '()))
    (list (dotcl-cltl2:compiler-let ((*clet-flag* :seq))
            (push 1 seen)
            (push 2 seen)
            (clet-at-expansion))
          seen))
  (:seq (2 1)))

;;; ---- through a file ----

;; The expansion-time binding is baked into what the file compiles to, so a
;; function loaded from the FASL answers what the variable was WHILE COMPILING,
;; and a run-time read still sees the global. SBCL: (:FROM-FASL :OUTER).
(deftest-compiled-only compiler-let.fasl-bakes-the-expansion-time-binding
  (let ((src "clet-tmp.lisp")
        (fasl "clet-tmp.fasl"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede)
            (format s "(defun clet-from-fasl ()~%")
            (format s "  (dotcl-cltl2:compiler-let ((*clet-flag* :from-fasl))~%")
            (format s "    (list (clet-at-expansion) (clet-at-runtime))))~%"))
          (compile-file src :output-file fasl)
          (load fasl)
          (list (funcall 'clet-from-fasl) *clet-flag*))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))
      (fmakunbound 'clet-from-fasl)))
  ((:from-fasl :outer) :outer))
