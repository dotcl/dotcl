;;; xref (who-calls): the compiler records caller->callee edges for every
;;; named call and #'name reference while a defun body compiles, and plants a
;;; load-time registration so the table also rebuilds on fasl load.
;;; dotcl:who-calls returns callers of a name; dotcl:who-is-called-by returns
;;; the callees recorded for a definition.
;;;
;;; Every test here is compiled-only: a build without a compiler records no edges
;;; at all, so the who-calls tests fail and the shadowing test (which asserts an
;;; ABSENCE) would pass for the wrong reason.

(defun %xref-t-callee-a (x) (1+ x))
(defun %xref-t-callee-b (x) (1- x))
(defun %xref-t-caller-direct (x) (%xref-t-callee-a x))
(defun %xref-t-caller-indirect (lst) (mapcar #'%xref-t-callee-a lst))
(defun %xref-t-caller-nested (lst)
  (funcall (lambda (l) (mapcar #'%xref-t-callee-b l)) lst))
(defun %xref-t-shadow ()
  (flet ((%xref-t-callee-a (x) x))
    (%xref-t-callee-a 1)))

(deftest-compiled-only xref-who-calls-direct-and-function-ref
  (sort (mapcar #'symbol-name (dotcl:who-calls '%xref-t-callee-a)) #'string<)
  ("%XREF-T-CALLER-DIRECT" "%XREF-T-CALLER-INDIRECT"))

;;; a call inside a nested lambda attributes to the enclosing defun
(deftest-compiled-only xref-who-calls-nested-lambda
  (mapcar #'symbol-name (dotcl:who-calls '%xref-t-callee-b))
  ("%XREF-T-CALLER-NESTED"))

(deftest-compiled-only xref-who-is-called-by
  (mapcar #'symbol-name (dotcl:who-is-called-by '%xref-t-caller-direct))
  ("%XREF-T-CALLEE-A"))

;;; a local flet call shadowing a global name is NOT an edge to the global
(deftest-compiled-only xref-flet-shadow-not-recorded
  (member "%XREF-T-SHADOW"
          (mapcar #'symbol-name (dotcl:who-calls '%xref-t-callee-a))
          :test #'string=)
  nil)

;;; redefinition replaces the caller's edge set (stale edges drop)
(deftest-compiled-only xref-redefinition-replaces-edges
  (progn
    (defun %xref-t-redef (x) (%xref-t-callee-a x))
    (let ((before (and (member '%xref-t-redef (dotcl:who-calls '%xref-t-callee-a)) t)))
      (defun %xref-t-redef (x) (%xref-t-callee-b x))
      (list before
            (and (member '%xref-t-redef (dotcl:who-calls '%xref-t-callee-a)) t)
            (and (member '%xref-t-redef (dotcl:who-calls '%xref-t-callee-b)) t))))
  (t nil t))

;;; Global variables and macros a definition uses: who-references (reads),
;;; who-sets (SETQ / SETF of the variable), who-binds (a special binding by
;;; LET, LET* or a lambda list) and who-macroexpands (a global macro).
(defvar *xref-t-var* 1)
(defmacro %xref-t-mac (x) `(list ,x))
(defun %xref-t-reader () (%xref-t-mac *xref-t-var*))
(defun %xref-t-setter () (setq *xref-t-var* 2) (setf *xref-t-var* 3))
(defun %xref-t-binder () (let ((*xref-t-var* 4)) (%xref-t-reader)))
(defun %xref-t-binder* () (let* ((a 1) (*xref-t-var* a)) (%xref-t-reader)))
(defun %xref-t-param-binder (*xref-t-var*) (%xref-t-reader))

(defun %xref-t-names (names)
  (sort (mapcar (lambda (n) (if (symbolp n) (symbol-name n) (prin1-to-string n)))
                names)
        #'string<))

(deftest-compiled-only xref-who-references
  (%xref-t-names (dotcl:who-references '*xref-t-var*))
  ("%XREF-T-READER"))

(deftest-compiled-only xref-who-sets
  (%xref-t-names (dotcl:who-sets '*xref-t-var*))
  ("%XREF-T-SETTER"))

(deftest-compiled-only xref-who-binds
  (%xref-t-names (dotcl:who-binds '*xref-t-var*))
  ("%XREF-T-BINDER" "%XREF-T-BINDER*" "%XREF-T-PARAM-BINDER"))

(deftest-compiled-only xref-who-macroexpands
  (%xref-t-names (dotcl:who-macroexpands '%xref-t-mac))
  ("%XREF-T-READER"))

;;; a local variable of the same name as a global one is not a reference
(defun %xref-t-shadow-var (xref-t-local) xref-t-local)
(deftest-compiled-only xref-lexical-not-a-reference
  (dotcl:who-references 'xref-t-local)
  nil)

;;; DEFMETHOD bodies are recorded under (DEFMETHOD name qualifier... (specializer...))
(defgeneric %xref-t-gf (x))
(defmethod %xref-t-gf ((x integer)) (%xref-t-callee-a *xref-t-var*))
(defmethod %xref-t-gf :around ((x string)) (setq *xref-t-var* x) (call-next-method))

(deftest-compiled-only xref-defmethod-caller
  (list (and (member '(defmethod %xref-t-gf (integer)) (dotcl:who-calls '%xref-t-callee-a)
                     :test #'equal) t)
        (and (member '(defmethod %xref-t-gf (integer)) (dotcl:who-references '*xref-t-var*)
                     :test #'equal) t)
        (and (member '(defmethod %xref-t-gf :around (string)) (dotcl:who-sets '*xref-t-var*)
                     :test #'equal) t)
        (let ((*xref-t-var* 6)) (%xref-t-gf 0)))
  (t t t 7))

;;; redefinition replaces the uses too
(deftest-compiled-only xref-redefinition-replaces-uses
  (progn
    (defun %xref-t-redef-use () *xref-t-var*)
    (let ((before (and (member '%xref-t-redef-use (dotcl:who-references '*xref-t-var*)) t)))
      (defun %xref-t-redef-use () (setq *xref-t-var* 1))
      (list before
            (and (member '%xref-t-redef-use (dotcl:who-references '*xref-t-var*)) t)
            (and (member '%xref-t-redef-use (dotcl:who-sets '*xref-t-var*)) t))))
  (t nil t))

;;; a registration without uses (the two-argument form) clears them
(deftest-compiled-only xref-note-two-arguments
  (progn
    (dotcl::%xref-note '%xref-t-noted '(%xref-t-callee-a) '(:references (*xref-t-var*)))
    (let ((before (and (member '%xref-t-noted (dotcl:who-references '*xref-t-var*)) t)))
      (dotcl::%xref-note '%xref-t-noted '(%xref-t-callee-b))
      (list before
            (and (member '%xref-t-noted (dotcl:who-references '*xref-t-var*)) t)
            (and (member '%xref-t-noted (dotcl:who-calls '%xref-t-callee-b)) t))))
  (t nil t))

;;; *record-xref* nil: nothing is recorded
(deftest-compiled-only xref-record-off
  (let ((dotcl:*record-xref* nil))
    (compile '%xref-t-unrecorded '(lambda () (%xref-t-callee-a *xref-t-var*)))
    (eval '(defun %xref-t-unrecorded-2 () (%xref-t-callee-a *xref-t-var*)))
    (list (member '%xref-t-unrecorded-2 (dotcl:who-calls '%xref-t-callee-a))
          (member '%xref-t-unrecorded-2 (dotcl:who-references '*xref-t-var*))))
  (nil nil))

;;; what an inline function's body uses is recorded for that function, not
;;; for each caller its body is substituted into
(defvar *xref-t-var2* 1)
(declaim (inline %xref-t-inl))
(defun %xref-t-inl () (%xref-t-callee-b *xref-t-var2*))
(defun %xref-t-uses-inl () (%xref-t-inl))

(deftest-compiled-only xref-inline-body-not-attributed-to-caller
  (list (%xref-t-names (dotcl:who-references '*xref-t-var2*))
        (and (member '%xref-t-uses-inl (dotcl:who-calls '%xref-t-inl)) t)
        (and (member '%xref-t-uses-inl (dotcl:who-calls '%xref-t-callee-b)) t))
  (("%XREF-T-INL") t nil))

;;; standard functions and macros the compiler lowers itself (an IF test, a
;;; fixnum lane, a form compiled for effect) are recorded as for any call;
;;; a binding named like a function is not a call
(defun %xref-t-std (a)
  (declare (fixnum a))
  (when (plusp a) (print (+ a 1) (make-broadcast-stream)))
  (dolist (x '(1)) (car (list x))))
(defun %xref-t-bind-name () (let ((list 1)) (1+ list)))

(deftest-compiled-only xref-standard-functions-and-macros
  (flet ((in (name list) (and (member name list :test #'equal) t)))
    (list (in '%xref-t-std (dotcl:who-calls 'plusp))
          (in '%xref-t-std (dotcl:who-calls 'print))
          (in '%xref-t-std (dotcl:who-calls '+))
          (in '%xref-t-std (dotcl:who-calls 'car))
          (in '%xref-t-std (dotcl:who-macroexpands 'when))
          (in '%xref-t-std (dotcl:who-macroexpands 'dolist))
          (in 'declare (dotcl:who-is-called-by '%xref-t-std))
          (in '%xref-t-bind-name (dotcl:who-calls 'list))
          (in '%xref-t-bind-name (dotcl:who-calls '1+))))
  (t t t t t t nil nil t))

;;; *record-xref-standard* nil leaves out what the compiler lowers itself
(deftest-compiled-only xref-record-standard-off
  (let ((dotcl:*record-xref-standard* nil))
    (compile '%xref-t-no-std '(lambda (a) (when a (car a)) (%xref-t-callee-a 1)))
    (list (and (member '%xref-t-no-std (dotcl:who-calls 'car)) t)
          (and (member '%xref-t-no-std (dotcl:who-macroexpands 'when)) t)
          (and (member '%xref-t-no-std (dotcl:who-calls '%xref-t-callee-a)) t)))
  (nil nil t))

;;; a macro's expander body is recorded as (macro-function name), and COMPILE
;;; records under the name it is given, or (lambda lambda-list ...) for NIL
(defmacro %xref-t-mac2 (x) (%xref-t-callee-a 1) `(list ,x))

(deftest-compiled-only xref-macro-function-and-compile
  (progn
    (compile '%xref-t-compiled '(lambda () (%xref-t-callee-b 2)))
    (compile nil '(lambda (%xref-t-arg) (%xref-t-callee-b %xref-t-arg)))
    (let ((callers (dotcl:who-calls '%xref-t-callee-b)))
      (list (and (member '(macro-function %xref-t-mac2) (dotcl:who-calls '%xref-t-callee-a)
                         :test #'equal)
                 t)
            (and (member '%xref-t-compiled callers) t)
            (and (find-if (lambda (c) (and (consp c) (eq (car c) 'lambda)
                                           (equal (second c) '(%xref-t-arg))))
                          callers)
                 t))))
  (t t t))

;;; a top level form of a compiled file is recorded as (:top-level-form file
;;; line); the definers there (DEFUN, DEFVAR) and what their expansions add
;;; are not
(deftest-emitting-only xref-top-level-form
  (let ((src (regression-temp-file "xref-top.lisp")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defvar cl-user::*xref-t-top* (cl-user::%xref-t-callee-a 1))~%~
                 (cl-user::%xref-t-callee-b 2)~%~
                 (defun cl-user::%xref-t-top-fn () 3)~%"))
    (load (compile-file src))
    (let ((tops (remove-if-not (lambda (c) (and (consp c) (eq (car c) :top-level-form)
                                                (search "xref-top" (second c))))
                               (append (dotcl:who-calls '%xref-t-callee-a)
                                       (dotcl:who-calls '%xref-t-callee-b)))))
      (list (sort (mapcar #'third tops) #'<)
            (some (lambda (c) (and (consp c) (eq (car c) :top-level-form)
                                   (search "xref-top" (second c))))
                  (dotcl:who-macroexpands 'defun)))))
  ((1 2) nil))

;;; the accessors, constructor and predicate a DEFCLASS, DEFINE-CONDITION or
;;; DEFSTRUCT defines are not calls of that top level form; a call written in
;;; a later top level form is
(deftest-emitting-only xref-top-level-definer-accessors
  (let ((src (regression-temp-file "xref-top-acc.lisp")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defclass cl-user::%xref-t-w () ((side :reader cl-user::%xref-t-side :initform 1)))~%~
                 (define-condition cl-user::%xref-t-c (error) ((why :reader cl-user::%xref-t-why)))~%~
                 (defstruct (cl-user::%xref-t-pt (:constructor cl-user::%xref-t-mk) (:predicate cl-user::%xref-t-is)) x)~%~
                 (defvar cl-user::*xref-t-side* (cl-user::%xref-t-side (make-instance 'cl-user::%xref-t-w)))~%"))
    (load (compile-file src))
    (flet ((lines (name)
             (sort (loop for c in (dotcl:who-calls name)
                         when (and (consp c) (eq (car c) :top-level-form)
                                   (search "xref-top-acc" (second c)))
                           collect (third c))
                   #'<)))
      (list (lines '%xref-t-side) (lines '%xref-t-why)
            (lines '%xref-t-mk) (lines '%xref-t-is))))
  ((4) nil nil nil))

;;; COMPILE of an anonymous lambda registers its edges at once; the queue of
;;; registrations that loading uses must not grow with the number of compiles.
(deftest-compiled-only xref.compile-loop-keeps-queue-bounded
  (progn
    (dotimes (i 12000) (compile nil '(lambda (x) (1+ x))))
    (< (length dotcl-internal::*xref-pending*) 9000))
  t)
