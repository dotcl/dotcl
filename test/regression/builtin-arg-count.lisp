;;; A call to a standard or dotcl function with too few or too many arguments
;;; signals PROGRAM-ERROR (CLHS 3.5.1.2, 3.5.1.3), for every function in the
;;; lambda list tables (compiler/cl-lambda-lists.lisp, ext-lambda-lists.lisp).
;;; Uses CL-LL-SHAPE from cl-builtin-lambda-lists.lisp.

;; Not called: they end the process, enter the debugger or a terminal UI, or
;; wait. Their counts are checked against the table below instead.
(defparameter *arg-count-not-called*
  '("QUIT" "EXIT" "SAVE-APPLICATION" "INVOKE-DEBUGGER" "ED" "INSPECT" "DRIBBLE" "SLEEP"
    "GC" "MAKE-THREAD" "UI-INVOKE" "UI-POST"))

;; (symbol min max) for each function in the tables; MAX is NIL for &rest / &key
;; (an extra argument to a &key function is a keyword argument, 3.5.1.6, not a
;; count error).
(defun arg-count-table-functions ()
  (let ((r '()))
    (dolist (section (cons (cons "COMMON-LISP" (funcall dotcl-internal::*cl-builtin-lambda-lists*))
                           (funcall dotcl-internal::*ext-builtin-lambda-lists*)))
      (let ((pkg (find-package (first section))))
        (when pkg
          (dolist (entry (rest section))
            (multiple-value-bind (s status) (find-symbol (first entry) pkg)
              (when (and (eq status :external) (eq (symbol-package s) pkg) (fboundp s)
                         (not (macro-function s)) (not (special-operator-p s)))
                (let ((ll (dotcl:function-lambda-list s)))
                  (destructuring-bind (req opt rest keys aok) (cl-ll-shape ll)
                    (declare (ignore keys aok))
                    (push (list s req (and (not rest) (not (member '&key ll)) (+ req opt))) r)))))))))
    (nreverse r)))

;; What calling FN with NARGS NILs does: :PROGRAM-ERROR, another condition's
;; type, or :RETURNED.
(defun arg-count-outcome (caller fn nargs)
  (block call
    (let ((*debugger-hook* (lambda (c h) (declare (ignore h))
                             (return-from call (list :debugger (type-of c))))))
      (handler-case
          (restart-case (progn (funcall caller fn (make-list nargs)) :returned)
            (abort (&rest r) (declare (ignore r)) :abort))
        (program-error () :program-error)
        (serious-condition (c) (type-of c))))))

;; The calls whose outcome is not PROGRAM-ERROR, as (symbol nargs outcome).
(defun arg-count-wrong-calls (caller)
  (let ((bad '()))
    (dolist (e (arg-count-table-functions) (nreverse bad))
      (destructuring-bind (s min max) e
        (unless (member (symbol-name s) *arg-count-not-called* :test #'string=)
          (let ((fn (fdefinition s)))
            (dolist (n (append (and (> min 0) (list (1- min))) (and max (list (1+ max)))))
              (let ((o (arg-count-outcome caller fn n)))
                (unless (eq o :program-error) (push (list s n o) bad))))))))))

(deftest builtin-arg-count-apply
  (arg-count-wrong-calls (lambda (fn args) (apply fn args)))
  nil)

;; Compiled FUNCALL with a fixed argument count goes through the direct
;; per-arity entries, not the args-array one APPLY takes.
(defparameter *arg-count-callers*
  (when (emitting-mode-p)
    (coerce (loop for n from 0 to 20
                  collect (compile nil `(lambda (f) (funcall f ,@(make-list n)))))
            'vector)))

(deftest-emitting-only builtin-arg-count-compiled-funcall
  (arg-count-wrong-calls (lambda (fn args) (funcall (aref *arg-count-callers* (length args)) fn)))
  nil)

;; The counts a C# function was registered with agree with its table entry.
(deftest builtin-arg-count-registered-matches-table
  (let ((n 0) (bad '()))
    (dolist (e (arg-count-table-functions))
      (destructuring-bind (s min max) e
        (let ((registered (dotcl-internal::%function-arg-count (fdefinition s))))
          (when registered
            (incf n)
            (unless (equal registered (list min max)) (push (list s registered) bad))))))
    (list (> n 140) bad))
  (t nil))

(deftest builtin-arg-count-message
  (handler-case (funcall 'float-radix 1.0 2)
    (program-error (c) (princ-to-string c)))
  "FLOAT-RADIX: too many arguments: 2 (expected 1)")
