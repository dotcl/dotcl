;;; UNTRACE restores only a live trace wrapper, not a redefined or unbound name.

(declaim (notinline tuf-f))

(defun tuf-compiled-caller (a) (list (tuf-f a)))

(let ((dotcl:*evaluator-mode* :interpret))
  (eval '(defun tuf-interpreted-caller (a) (list (tuf-f a)))))

(defvar *tuf-callers* (list #'tuf-compiled-caller #'tuf-interpreted-caller))

(defun %tuf-define (delta)
  (eval `(defun tuf-f (a) (+ a ,delta))))

(defun %tuf-reset ()
  (untrace)
  (%tuf-define 50))

(defun %tuf-trace () (eval '(trace tuf-f)))
(defun %tuf-untrace () (eval '(untrace tuf-f)))

(defun %tuf-traced-call (caller a)
  (let ((out (make-string-output-stream)))
    (list (let ((*trace-output* out)) (funcall caller a))
          (plusp (length (get-output-stream-string out))))))

(defun %tuf-call-undef (caller a)
  (handler-case (funcall caller a)
    (undefined-function (c) (list :undefined (cell-error-name c)))))

(defun %tuf-run (scenario)
  (loop for caller in *tuf-callers*
        collect (progn (%tuf-reset) (funcall scenario caller))))

(defun %tuf-scenario-exact (caller)
  (list (funcall caller 1)
        (progn (%tuf-trace) (%tuf-traced-call caller 1))
        (progn (%tuf-untrace) (funcall caller 1))
        (progn (%tuf-define 7) (funcall caller 1))
        (progn (fmakunbound 'tuf-f) (%tuf-call-undef caller 1))
        (fboundp 'tuf-f)))

(defun %tuf-scenario-redefine (caller)
  (%tuf-trace)
  (%tuf-define 2)
  (list (funcall caller 1)
        (progn (%tuf-untrace) (funcall caller 1))
        (fboundp 'tuf-f)))

(defun %tuf-scenario-unbound (caller)
  (%tuf-trace)
  (fmakunbound 'tuf-f)
  (list (fboundp 'tuf-f)
        (%tuf-call-undef caller 1)
        (progn (%tuf-untrace) (fboundp 'tuf-f))
        (%tuf-call-undef caller 1)))

(defun %tuf-scenario-unbound-redefine (caller)
  (%tuf-trace)
  (fmakunbound 'tuf-f)
  (%tuf-define 4)
  (list (funcall caller 1)
        (progn (%tuf-untrace) (funcall caller 1))
        (fboundp 'tuf-f)))

(defun %tuf-scenario-retrace-unbound (caller)
  (%tuf-trace)
  (fmakunbound 'tuf-f)
  (%tuf-define 4)
  (%tuf-trace)
  (list (%tuf-traced-call caller 1)
        (progn (%tuf-untrace) (funcall caller 1))
        (fboundp 'tuf-f)))

(defun %tuf-scenario-retrace-redefined (caller)
  (%tuf-trace)
  (%tuf-define 3)
  (%tuf-trace)
  (list (%tuf-traced-call caller 1)
        (progn (%tuf-untrace) (funcall caller 1))
        (fboundp 'tuf-f)))

(defun %tuf-setf-reset ()
  (untrace)
  (eval '(progn
           (defun tuf-slot (o) (car o))
           (defun (setf tuf-slot) (v o) (setf (car o) (+ v 100))))))

(defun %tuf-scenario-setf-unbound ()
  (%tuf-setf-reset)
  (eval '(trace (setf tuf-slot)))
  (fmakunbound '(setf tuf-slot))
  (let ((o (list 0)))
    (list (fboundp '(setf tuf-slot))
          (handler-case (funcall (fdefinition '(setf tuf-slot)) 1 o)
            (undefined-function (c) (list :undefined (cell-error-name c))))
          (progn (eval '(untrace (setf tuf-slot))) (fboundp '(setf tuf-slot)))
          (progn (eval '(defun (setf tuf-slot) (v o) (setf (car o) (+ v 200))))
                 (funcall (fdefinition '(setf tuf-slot)) 1 o)
                 (car o)))))

(deftest trace-untrace-fmakunbound.exact
  (equal (%tuf-run #'%tuf-scenario-exact)
         (list '((51) ((51) t) (51) (8) (:undefined tuf-f) nil)
               '((51) ((51) t) (51) (8) (:undefined tuf-f) nil)))
  t)

(deftest trace-untrace-fmakunbound.redefine-while-traced
  (equal (%tuf-run #'%tuf-scenario-redefine)
         '(((3) (3) t) ((3) (3) t)))
  t)

(deftest trace-untrace-fmakunbound.unbound-while-traced
  (equal (%tuf-run #'%tuf-scenario-unbound)
         (list '(nil (:undefined tuf-f) nil (:undefined tuf-f))
               '(nil (:undefined tuf-f) nil (:undefined tuf-f))))
  t)

(deftest trace-untrace-fmakunbound.unbound-redefine-untrace
  (equal (%tuf-run #'%tuf-scenario-unbound-redefine)
         '(((5) (5) t) ((5) (5) t)))
  t)

(deftest trace-untrace-fmakunbound.retrace-after-unbound
  (equal (%tuf-run #'%tuf-scenario-retrace-unbound)
         '((((5) t) (5) t) (((5) t) (5) t)))
  t)

(deftest trace-untrace-fmakunbound.retrace-after-redefine
  (equal (%tuf-run #'%tuf-scenario-retrace-redefined)
         '((((4) t) (4) t) (((4) t) (4) t)))
  t)

(deftest trace-untrace-fmakunbound.setf-unbound
  (%tuf-scenario-setf-unbound)
  (nil (:undefined (setf tuf-slot)) nil 201))
